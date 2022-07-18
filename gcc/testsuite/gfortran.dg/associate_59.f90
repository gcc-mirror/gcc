! { dg-do compile }
! PR fortran/103590 - ICE: find_array_spec(): Missing spec 
! Contributed by G.Steinmetz

program p
  associate (a => 1)
    print *, [character(a(1)) :: '1'] ! { dg-error "Scalar INTEGER expression" }
  end associate
end
