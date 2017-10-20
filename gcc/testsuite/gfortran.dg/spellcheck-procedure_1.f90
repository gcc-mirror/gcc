! { dg-do compile }
! test levenshtein based spelling suggestions

module mymod1
  implicit none
  contains
    function something_else (iarg1)
      integer :: something_else
      integer, intent(in) :: iarg1
      something_else = iarg1 + 42
    end function something_else
    function add_fourtytwo (iarg1)
      integer :: add_fourtytwo
      integer, intent(in) :: iarg1
      add_fourtytwo = iarg1 + 42
    end function add_fourtytwo
end module mymod1

function myadd(iarg1, iarg2)
  implicit none
  integer :: myadd
  integer, intent(in) :: iarg1, iarg2
  myadd = iarg1 + iarg2
end function myadd

program spellchekc
  use mymod1, something_good => something_else
  implicit none

  integer :: myadd, i, j, myvar
  i = 0
  j = 0

  j = something_goof(j) ! { dg-error "no IMPLICIT type; did you mean .something_good.\\?" }
  j = myaddd(i, j) ! { dg-error "no IMPLICIT type; did you mean .myadd.\\?" }
  if (j /= 42) call abort
  j = add_fourtytow(i, j) ! { dg-error "no IMPLICIT type; did you mean .add_fourtytwo.\\?" }
  myval = myadd(i, j) ! { dg-error "no IMPLICIT type; did you mean .myvar.\\?" }
  if (j /= 42 * 2) call abort

end program spellchekc
