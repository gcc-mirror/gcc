! { dg-do compile }
! test levenshtein based spelling suggestions

module mymod1
  implicit none
  contains
    function something_good (iarg1)
      integer :: something_good
      integer, intent(in) :: iarg1
      something_good = iarg1 + 42
    end function something_good
end module mymod1

program spellchekc
  use mymod1
  implicit none

  interface operator (.mywrong.)
    module procedure something_wring ! { dg-error "Procedure .something_wring. in operator interface .mywrong. at .1. is neither function nor subroutine; did you mean .something_good.\\?|User operator procedure .something_wring. at .1. must be a FUNCTION" }
  end interface

  interface operator (.mygood.)
    module procedure something_good
  end interface

  integer :: i, j, added
  i = 0
  j = 0
  added = .mygoof. j ! { dg-error "Unknown operator .mygoof. at .1.; did you mean .mygood.\\?" }
end program spellchekc
