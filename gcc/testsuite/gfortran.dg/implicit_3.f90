! { dg-do compile }
! Verify that INTERFACEs don't inherit the implicit types of the
! surrounding namespace.
implicit complex (i-k)

interface
   function f(k,l)
     ! k should be default INTEGER 
     dimension l(k)
   end function f
end interface
end
