! { dg-do compile }
! { dg-options "-O2" }
program lh
  call za(0)
  call za(0)
contains
  subroutine za(wl)
    integer wl
    wl = 1
  end subroutine za
end program lh
