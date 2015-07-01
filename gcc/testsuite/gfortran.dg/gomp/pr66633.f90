! PR middle-end/66633
! Testcase by Andrew Benson <abensonca@gmail.com>

! { dg-do compile }
! { dg-options "-O0 -fopenmp" }

module spls
contains
  function spl()
    !$omp parallel
    write (0,*) igrt(fli)
    !$omp end parallel
  contains
    double precision function fli()
    end function fli
  end function spl
end module spls
