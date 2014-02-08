! { dg-do run }
!
! PR fortran/60066
!
! Contributed by F Martinez Fadrique  <fmartinez@gmv.com>
!
! Fixed by the patch for PR59906 but adds another, different test.
!
module m_assertion_character
  implicit none
  type :: t_assertion_character
    character(len=8) :: name
  contains
    procedure :: assertion_character
    procedure :: write => assertion_array_write
  end type t_assertion_character
contains
  elemental subroutine assertion_character( ast, name )
    class(t_assertion_character), intent(out) :: ast
    character(len=*), intent(in) :: name
    ast%name = name
  end subroutine assertion_character
  subroutine assertion_array_write( ast, unit )
    class(t_assertion_character), intent(in) :: ast
    character(*), intent(inOUT) :: unit
    write(unit,*) trim (unit(2:len(unit)))//trim (ast%name)
  end subroutine assertion_array_write
end module m_assertion_character

module m_assertion_array_character
  use m_assertion_character
  implicit none
  type :: t_assertion_array_character
    type(t_assertion_character), dimension(:), allocatable :: rast
  contains
    procedure :: assertion_array_character
    procedure :: write => assertion_array_character_write
  end type t_assertion_array_character
contains
  pure subroutine assertion_array_character( ast, name, nast )
    class(t_assertion_array_character), intent(out) :: ast
    character(len=*), intent(in) :: name
    integer, intent(in) :: nast
    integer :: i
    allocate ( ast%rast(nast) )
    call ast%rast%assertion_character ( name )
  end subroutine assertion_array_character
  subroutine assertion_array_character_write( ast, unit )
    class(t_assertion_array_character), intent(in) :: ast
    CHARACTER(*), intent(inOUT) :: unit
    integer :: i
    do i = 1, size (ast%rast)
      call ast%rast(i)%write (unit)
    end do
  end subroutine assertion_array_character_write
end module m_assertion_array_character

program main
  use m_assertion_array_character
  implicit none
  type(t_assertion_array_character) :: ast
  character(len=8) :: name
  character (26) :: line = ''
  name = 'test'
  call ast%assertion_array_character ( name, 5 )
  call ast%write (line)
  if (line(2:len (line)) .ne. "testtesttesttesttest") call abort
end program main
