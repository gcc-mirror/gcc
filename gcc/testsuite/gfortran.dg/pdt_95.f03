! { dg-do run }
!
! Test the fix for PR125690, where an ICE occurred on deallocating the
! PDT string components.
!
! Contributed by David Binderman  <dcb314@hotmail.com>
!            and Paul Thomas  <pault@gcc.gnu.org> for tests in comment #5.
!
module m
  integer, parameter :: def_kind = selected_char_kind('ascii')
  integer, parameter :: unicode_kind = selected_char_kind('ISO_10646')
  character(kind = unicode_kind,len = 7) :: cc1(5) = ['a','b','c','d','e']
  character(kind = def_kind, len = 5) :: str(3) = ['abcde','fghij','klmno']
end module

module m5
! Declarations for comment #5
  type ux(l,m)
    integer, len :: l,m
    character(len=l) :: x(m)
    integer :: tag
  end type
  type uy(l)
    integer, len :: l
    character(len=l) :: x(3)
    integer :: tag
  end type
  type uz(l)
    integer, len :: l
    character(len=l), allocatable :: x(:)
    integer :: tag
  end type
  integer :: n = 5
end module

program p
  use m
  use m5
  call sub1(cc1) ! Check ascii kind
  call sub2(cc1) ! Check unicode kind
! Tests from comment #5
  block
    type(uy(n)) :: obj
    obj%tag = 42
    obj%x(1) = 'aaaaa'
    obj%x(2) = 'bbbbb'
    obj%x(3) = 'ccccc'
    if (len(obj%x) /= 5) stop 101
    if (size(obj%x) /= 3) stop 102
    if (obj%x(1) /= 'aaaaa') stop 103
    if (obj%x(2) /= 'bbbbb') stop 104
    if (obj%x(3) /= 'ccccc') stop 105
    if (obj%tag /= 42) stop 106
  end block
  call foo(n)
  call bar(n,3)
  call foobar(n)
contains
  subroutine sub1(cc1)
    character(kind = 4,len = 7)::cc1(5)
    Type ty(k1)
      Integer,kind  :: k1
      character(kind= k1,len = len(cc1)+k1) :: ch(len(cc1)-5) 
      character(kind= k1,len = 2+len(cc1)+k1) :: c(len(cc1)*5) 
      character(kind= k1,len = kind(cc1)+k1) :: ch1(len(cc1))
      character(kind= k1,len = kind(cc1)+2) :: c2(kind(cc1))
      character(kind= k1,len = len(cc1)+2) :: ch2(kind(cc1))
    End type
    Type(ty(def_kind)) :: obj
    if(len(obj%ch) .ne. len(cc1) + def_kind) stop 1
    if(len(obj%c) .ne. 2 + len(cc1) + def_kind) stop 2
    if(len(obj%ch1) .ne. kind(cc1) + def_kind)  stop 3
    if(len(obj%c2) .ne. 6)  stop 4
    if(len(obj%ch2) .ne. 9)  stop 5
    if(ubound(obj%ch,1) .ne. 2)  stop 6
    if(ubound(obj%ch1,1) .ne. 7)  stop 7
    if(ubound(obj%c,1) .ne. 35)  stop 8
    if(ubound(obj%ch2,1) .ne. 4)  stop 9
    if(ubound(obj%c2,1) .ne. 4)  stop 10
  end subroutine
  subroutine sub2(cc1)
    character(kind = 4,len = 7)::cc1(5)
    Type ty(k1)
      Integer,kind  :: k1
      character(kind= k1,len = len(cc1)+k1) :: ch(len(cc1)-5) 
      character(kind= k1,len = 2+len(cc1)+k1) :: c(len(cc1)*5) 
      character(kind= k1,len = kind(cc1)+k1) :: ch1(len(cc1))
      character(kind= k1,len = kind(cc1)+2) :: c2(kind(cc1))
      character(kind= k1,len = len(cc1)+2) :: ch2(kind(cc1))
    End type
    Type(ty(unicode_kind)) :: obj
    if(len(obj%ch) .ne. len(cc1) + unicode_kind) stop 11
    if(len(obj%c) .ne. 2 + len(cc1) + unicode_kind) stop 12
    if(len(obj%ch1) .ne. kind(cc1) + unicode_kind)  stop 13
    if(len(obj%c2) .ne. 6)  stop 14
    if(len(obj%ch2) .ne. 9)  stop 15
    if(ubound(obj%ch,1) .ne. 2)  stop 16
    if(ubound(obj%ch1,1) .ne. 7)  stop 17
    if(ubound(obj%c,1) .ne. 35)  stop 18
    if(ubound(obj%ch2,1) .ne. 4)  stop 19
    if(ubound(obj%c2,1) .ne. 4)  stop 20
  end subroutine

! Comment #5 tests
  subroutine foo(i)
    integer :: i
    character(kind = def_kind, len = 3 * i) :: buffer
    type(uy(i)) :: obj
    obj%tag = 42
    obj%x(1) = 'aaaaa'
    obj%x(2) = 'bbbbb'
    obj%x(3) = 'ccccc'
    if (len(obj%x) /= 5) stop 111
    if (size(obj%x) /= 3) stop 112
    if (obj%x(1) /= 'aaaaa') stop 113
    if (obj%x(2) /= 'bbbbb') stop 114
    if (obj%x(3) /= 'ccccc') stop 115
    if (obj%tag /= 42) stop 116
! It was noticed, while verifying the patch for this PR, that these PDT string
! components were not scalarizing correctly nor were they transferring to IO.
    obj%x = str
    if (any (obj%x /= str)) stop 117
    write (buffer, '(3a5)') obj%x
    if (buffer /= 'abcdefghijklmno') stop 118
   end
  subroutine bar(i,j)
    integer :: i, j
    character(kind = def_kind, len = j * i) :: buffer
    type(ux(i, j)) :: obj
    obj%tag = 42
    obj%x(1) = 'aaaaa'
    obj%x(2) = 'bbbbb'
    obj%x(3) = 'ccccc'
    if (len(obj%x) /= 5) stop 121
    if (size(obj%x) /= 3) stop 122
    if (obj%x(1) /= 'aaaaa') stop 123
    if (obj%x(2) /= 'bbbbb') stop 124
    if (obj%x(3) /= 'ccccc') stop 125
    if (obj%tag /= 42) stop 126
! It was noticed, while verifying the patch for this PR, that these PDT string
! components were not scalarizing correctly nor were they transferring to IO.
    obj%x = str
    if (any (obj%x /= str)) stop 127
    write (buffer, '(3a5)') obj%x
    if (buffer /= 'abcdefghijklmno') stop 128
   end
  subroutine foobar(i)
    integer :: i
    character(kind = def_kind, len = 3 * i) :: buffer
! Note that, with the present patch, the declaration produces
!      obj.x.data = 0B; twice because it is allocatable and a PDT string. 
    type(uz(i)) :: obj
    allocate (character(len=i) :: obj%x(3))
    obj%tag = 42
    obj%x(1) = 'aaaaa'
    obj%x(2) = 'bbbbb'
    obj%x(3) = 'ccccc'
    if (len(obj%x) /= 5) stop 131
    if (size(obj%x) /= 3) stop 132
    if (obj%x(1) /= 'aaaaa') stop 133
    if (obj%x(2) /= 'bbbbb') stop 134
    if (obj%x(3) /= 'ccccc') stop 135
    if (obj%tag /= 42) stop 136
    obj%x = str
! Allocatable pdt_string components were OK for scalariztion and IO.
    if (any (obj%x /= str)) stop 137
    write (buffer, '(3a5)') obj%x
    if (buffer /= 'abcdefghijklmno') stop 138
   end
End
