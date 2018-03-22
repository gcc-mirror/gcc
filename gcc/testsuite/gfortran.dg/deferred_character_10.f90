! { dg-do run }
!
! Checks that PR60593 is fixed (Revision: 214757)
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
! Main program added for this test.
!
module stringhelper_m

  implicit none

  type :: string_t
     character(:), allocatable :: string
  end type

  interface len
     function strlen(s) bind(c,name='strlen')
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: s
       integer(c_size_t) :: strlen
     end function
  end interface

  contains

    function C2FChar(c_charptr) result(res)
      use iso_c_binding
      type(c_ptr), intent(in) :: c_charptr
      character(:), allocatable :: res
      character(kind=c_char,len=1), pointer :: string_p(:)
      integer i, c_str_len
      c_str_len = int(len(c_charptr))
      call c_f_pointer(c_charptr, string_p, [c_str_len])
      allocate(character(c_str_len) :: res)
      forall (i = 1:c_str_len) res(i:i) = string_p(i)
    end function

end module

  use stringhelper_m
  use iso_c_binding
  implicit none
  type(c_ptr) :: cptr
  character(20), target :: str

  str = "abcdefghij"//char(0)
  cptr = c_loc (str)
  if (len (C2FChar (cptr)) .ne. 10) STOP 1
  if (C2FChar (cptr) .ne. "abcdefghij") STOP 2
end
