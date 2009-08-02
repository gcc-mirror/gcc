! { dg-do run }
! { dg-options "-std=legacy" }
!
! Tests passing of character array initialiser as actual argument.
! Fixes PR18109.
! Contributed by Paul Thomas pault@gcc.gnu.org  
program char_initialiser
  character*5, dimension(3) :: x
  character*5, dimension(:), pointer :: y
  x=(/"is Ja","ne Fo","nda  "/)
  call sfoo ("is Ja", x(1))
  call afoo ((/"is Ja","ne Fo","nda  "/), x)
  y => pfoo ((/"is Ja","ne Fo","nda  "/))
  call afoo (y, x)
contains
  subroutine sfoo(ch1, ch2)
     character*(*)               :: ch1, ch2
     if (ch1 /= ch2) call abort ()
  end subroutine sfoo
  subroutine afoo(ch1, ch2)
     character*(*), dimension(:) :: ch1, ch2
     if (any(ch1 /= ch2)) call abort ()
  end subroutine afoo
  function pfoo(ch2)
     character*5, dimension(:), target  :: ch2
     character*5, dimension(:), pointer :: pfoo
     pfoo => ch2
  end function pfoo
end program
