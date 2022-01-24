! { dg-additional-options -Wuninitialized }

type :: type1
  character(len=35,kind=4) :: a
end type type1

type :: type2
  character(len=35,kind=4), pointer :: b
end type type2

type :: aux1
  character(len=22,kind=4) :: y
end type aux1

type, extends(aux1) :: aux
  character(len=33,kind=4) :: x
end type aux

type :: type3
  class(aux), pointer :: c(:)
end type type3

type :: type4
  integer, pointer :: d(:)
end type type4

type :: type5
  type(aux1) :: e
end type type5

type :: type6
  type(aux1), pointer :: f
end type type6

type :: type7
  class(aux), pointer :: g
end type type7

type(type1) :: foo
type(type2) :: bar
type(type3) :: qux
type(type4) :: quux
type(type5) :: fred
type(type6) :: jim
type(type7) :: shiela

type(type1), pointer :: pfoo
type(type2), pointer :: pbar
type(type3), pointer :: pqux
type(type4), pointer :: pquux
type(type5), pointer :: pfred
type(type6), pointer :: pjim
type(type7), pointer :: pshiela

class(type1), pointer :: cfoo
class(type2), pointer :: cbar
class(type3), pointer :: cqux
class(type4), pointer :: cquux
class(type5), pointer :: cfred
class(type6), pointer :: cjim
class(type7), pointer :: cshiela

class(type1), allocatable :: acfoo
class(type2), allocatable :: acbar
class(type3), allocatable :: acqux
class(type4), allocatable :: acquux
class(type5), allocatable :: acfred
class(type6), allocatable :: acjim
class(type7), allocatable :: acshiela

!$acc enter data copyin(foo)
!$acc enter data copyin(foo%a)
!$acc enter data copyin(bar)
!$acc enter data copyin(bar%b)
!$acc enter data copyin(qux)
!$acc enter data copyin(qux%c)
!$acc enter data copyin(quux)
!$acc enter data copyin(quux%d)
!$acc enter data copyin(fred)
!$acc enter data copyin(fred%e)
!$acc enter data copyin(jim)
!$acc enter data copyin(jim%f)
!$acc enter data copyin(shiela)
!$acc enter data copyin(shiela%g)

!$acc enter data copyin(pfoo)
!$acc enter data copyin(pfoo%a)
!$acc enter data copyin(pbar)
!$acc enter data copyin(pbar%b)
!$acc enter data copyin(pqux)
!$acc enter data copyin(pqux%c)
!$acc enter data copyin(pquux)
!$acc enter data copyin(pquux%d)
!$acc enter data copyin(pfred)
!$acc enter data copyin(pfred%e)
!$acc enter data copyin(pjim)
!$acc enter data copyin(pjim%f)
!$acc enter data copyin(pshiela)
!$acc enter data copyin(pshiela%g)

!$acc enter data copyin(cfoo)
!$acc enter data copyin(cfoo%a)
!$acc enter data copyin(cbar)
!$acc enter data copyin(cbar%b)
!$acc enter data copyin(cqux)
!$acc enter data copyin(cqux%c)
!$acc enter data copyin(cquux)
!$acc enter data copyin(cquux%d)
!$acc enter data copyin(cfred)
!$acc enter data copyin(cfred%e)
!$acc enter data copyin(cjim)
!$acc enter data copyin(cjim%f)
!$acc enter data copyin(cshiela)
!$acc enter data copyin(cshiela%g)

!$acc enter data copyin(acfoo)
!$acc enter data copyin(acfoo%a)
!$acc enter data copyin(acbar)
!$acc enter data copyin(acbar%b)
!$acc enter data copyin(acqux)
!$acc enter data copyin(acqux%c)
!$acc enter data copyin(acquux)
!$acc enter data copyin(acquux%d)
!$acc enter data copyin(acfred)
!$acc enter data copyin(acfred%e)
!$acc enter data copyin(acjim)
!$acc enter data copyin(acjim%f)
!$acc enter data copyin(acshiela)
!$acc enter data copyin(acshiela%g)

end
