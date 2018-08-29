! { dg-do compile }
! test levenshtein based spelling suggestions
implicit none

!!!!!!!!!!!!!! structure tests !!!!!!!!!!!!!!
type type1
   real :: radius
   integer :: i
end type type1

type type2
  integer :: myint
  type(type1) :: mytype
end type type2

type type3
  type(type2) :: type_2
end type type3
type type4
  type(type3) :: type_3
end type type4

type(type1) :: t1
t1%radiuz = .0 ! { dg-error ".radiuz. at .1. is not a member of the .type1. structure; did you mean .radius.\\?" }
t1%x = .0 ! { dg-error ".x. at .1. is not a member of the .type1. structure" }
type(type2) :: t2
t2%mytape%radius = .0 ! { dg-error ".mytape. at .1. is not a member of the .type2. structure; did you mean .mytype.\\?" }
t2%mytype%radious = .0 ! { dg-error ".radious. at .1. is not a member of the .type1. structure; did you mean .radius.\\?" }
type(type4) :: t4
t4%type_3%type_2%mytype%radium = 88.0 ! { dg-error ".radium. at .1. is not a member of the .type1. structure; did you mean .radius.\\?" }

!!!!!!!!!!!!!! symbol tests !!!!!!!!!!!!!!
integer :: iarg1
iarg2 = 1 ! { dg-error "Symbol .iarg2. at .1. has no IMPLICIT type; did you mean .iarg1.\\?" }
end
