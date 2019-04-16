! Valid usage of 'external' procedures with OpenACC 'routine' directives.

! { dg-additional-options "-fdump-tree-optimized-raw" }

  subroutine test (x)
    implicit none
    integer, intent(inout) :: x
    !$acc routine (test)

    integer, external :: f_1
    !$acc routine (f_1)

    integer f_2 ! No explicit EXTERNAL attribute.
    !$acc routine (f_2)

    external s_1
    !$acc routine (s_1)

    ! 's_2' will be an external subroutine without explicit EXTERNAL
    ! attribute, but we don't have a handle for it yet...
    !!$acc routine (s_2) ..., so can't specify this, here.

    if (x < 1) then
       x = 1
    else
       x = x * x - 1 + f_1(f_2(x))
       call s_1(x)
       call s_2(x)
    end if
  end subroutine test

! { dg-final { scan-tree-dump-times "gimple_call" 4 "optimized" } }
! { dg-final { scan-tree-dump-times "gimple_call <f_1," 1 "optimized" } }
! { dg-final { scan-tree-dump-times "gimple_call <f_2," 1 "optimized" } }
! { dg-final { scan-tree-dump-times "gimple_call <s_1," 1 "optimized" } }
! { dg-final { scan-tree-dump-times "gimple_call <s_2," 1 "optimized" } }
