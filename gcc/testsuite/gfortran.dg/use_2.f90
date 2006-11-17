! { dg-do compile }
subroutine bar1
  usefoo ! { dg-error "Unclassifiable statement" }
end
