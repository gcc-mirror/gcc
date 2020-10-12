! { dg-do compile }
! { dg-additional-options "-Wno-analyzer-too-complex --param analyzer-max-svalue-depth=0" }

program n6
  integer :: ck(2,2)
  integer :: ac

  data ck /4 * 1/

  call x9()

contains
  subroutine x9()
    if (ck(2, 1) == 1) then
       ac = 1
    else
       ac = 0
    end if
  end subroutine x9
end program n6
