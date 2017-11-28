! { dg-do compile }
! Contributed by Joost VandeVondele
! test levenshtein based spelling suggestions for keyword arguments

module test
contains
  subroutine mysub(iarg1)
    integer :: iarg1
  end subroutine
end module

use test
call mysub(iarg=1) ! { dg-error "Keyword argument .iarg. at .1. is not in the procedure; did you mean .iarg1.\\?" }

end
