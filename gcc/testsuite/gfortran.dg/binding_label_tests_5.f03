! { dg-do compile }
module binding_label_tests_5
  use, intrinsic :: iso_c_binding
  
  interface
     subroutine sub0() bind(c, name='c_sub') ! Odd declaration but perfectly valid
     end subroutine sub0
     
     subroutine sub1() bind(c, name='c_sub') ! Ditto.
     end subroutine sub1
  end interface
end module binding_label_tests_5
