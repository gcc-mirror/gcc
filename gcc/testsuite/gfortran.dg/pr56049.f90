! { dg-do compile }
! { dg-options "-O3 -fdump-tree-optimized" }

program inline

    integer i
    integer a(8,8), b(8,8)

    a = 0
    do i = 1, 10000000
        call add(b, a, 1)
        a = b
    end do

    print *, a

contains

    subroutine add(b, a, o)
        integer, intent(inout) :: b(8,8)
        integer, intent(in) :: a(8,8), o
        b = a + o
    end subroutine add

end program inline

! Check there's no loop left, just two bb 2 in two functions.
! { dg-final { scan-tree-dump-times "<bb \[0-9\]*>" 2 "optimized" } }
! { dg-final { scan-tree-dump-times "<bb 2>" 2 "optimized" } }
