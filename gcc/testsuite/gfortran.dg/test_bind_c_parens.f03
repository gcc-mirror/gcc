! { dg-do compile }
module test_bind_c_parens
  interface
     subroutine sub bind(c) ! { dg-error "Missing required parentheses" }
     end subroutine sub ! { dg-error "Expecting END INTERFACE" }
  end interface
end module test_bind_c_parens 
