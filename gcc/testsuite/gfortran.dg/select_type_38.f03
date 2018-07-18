  type :: t1
  end type
  type, extends(t1) :: t2
  end type
  class(t1), pointer :: a
lab1: select type (a)
  end select lab1
lab1: select type (a)		! { dg-error "Duplicate construct label" }
  end select lab1		! { dg-error "Expecting END PROGRAM statement" }
end
