! { dg-do compile }
! { dg-options "-std=f2008" }
! PR36725 Compile time error for g0 edit descriptor
print '(g0.9)', 0.1 ! { dg-error "Specifying precision" }
end
