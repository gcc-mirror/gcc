! { dg-do compile }
! PR31251 Non-integer character length leads to segfault
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
  character(len=2.3) :: s ! { dg-error "must be of INTEGER type" }
  character(kind=1,len=4.3) :: t ! { dg-error "must be of INTEGER type" }
  character(len=,,7.2,kind=1) :: u ! { dg-error "Syntax error in CHARACTER declaration" }
  character(len=7,kind=2) :: v ! ! { dg-error "Kind 2 is not supported for CHARACTER" }
  character(kind=2) :: w ! ! { dg-error "Kind 2 is not supported for CHARACTER" }
  character(kind=2,len=7) :: x ! ! { dg-error "Kind 2 is not supported for CHARACTER" }
  end
