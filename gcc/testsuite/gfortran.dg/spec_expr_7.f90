! { dg-do run }
!
! PR 60777: [F03] RECURSIVE function rejected in specification expression
!
! Contributed by Vladimir Fuka <vladimir.fuka@gmail.com>

module recur
  implicit none
contains

  pure recursive function f(n) result(answer)
    integer, intent(in) :: n
    integer             :: answer
    if (n<2) then
      answer = 1
    else
      answer = f(n-1)*n
    end if
  end function

  pure function usef(n)
    integer,intent(in) :: n
    character(f(n))    :: usef
    usef = repeat('*',f(n))
  end function
end module

program testspecexpr
  use recur
  implicit none
  if (usef(1) /= '*')      call abort()
  if (usef(2) /= '**')     call abort()
  if (usef(3) /= '******') call abort()
end
