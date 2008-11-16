! { dg-do run }
! PR38119 - The scalarizer got the loop size wrong
! for the temporary coming from the call to 'same'.
!
! Contributed by Mikael Morin <mikael.morin@tele2.fr>
! based on a program by Vivek Rao.
!
module bar
  implicit none
  character(len = 2) :: c(1)
contains
  elemental function trim_append (xx,yy) result(xy)
    character (len = *), intent(in) :: xx,yy
    character (len = len (xx) + len (yy)) :: xy
    xy = trim (xx) // trim (yy)
  end function trim_append
  function same(xx) result(yy)
    character (len = *), intent(in) :: xx(:)
    character (len = len (xx))       :: yy(size (xx))
    yy = xx
  end function same
  subroutine xmain()
    c =  trim_append(["a"],same(["b"]))  ! The problem occurred here
  end subroutine xmain
end module bar
  use bar
  call xmain
  if (c(1) .ne. "ab") call abort
end
! { dg-final { cleanup-modules "bar" } }

