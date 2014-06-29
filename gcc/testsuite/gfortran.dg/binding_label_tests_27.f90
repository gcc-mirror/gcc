! { dg-do compile }

module p

  implicit none
  integer i1, i2, i3, i4, i5, i6, i7, i8, i9, i10

  character(len=*), parameter :: s = "toto"
  character(len=*), parameter :: t(2) = ["x", "y"]

  bind(c,name="   foo    ") :: i1
  bind(c, name=trim("Hello   ") // "There") :: i2
  bind(c, name=1_"name") :: i3
  bind(c, name=4_"") :: i4 ! { dg-error "scalar of default character kind" }
  bind(c, name=1) :: i5 ! { dg-error "scalar of default character kind" }
  bind(c, name=1.0) :: i6 ! { dg-error "scalar of default character kind" }
  bind(c, name=["","",""]) :: i7 ! { dg-error "scalar of default character kind" }
  bind(c, name=s) :: i8
  bind(c, name=t(2)) :: i9

end module

subroutine foobar(s)
  character(len=*) :: s
  integer :: i
  bind(c, name=s) :: i ! { dg-error "constant expression" }
end subroutine
