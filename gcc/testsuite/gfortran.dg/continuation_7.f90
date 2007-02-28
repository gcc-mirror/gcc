! { dg-do "compile" }
! { dg-options "-Wall -std=f95" }
! There should only two warnings be printed.
! PR fortran/30968
print *, "Foo bar&
        &Bar foo"
print *, "Foo bar&
        Bar foo" ! { dg-warning "Missing '&' in continued character constant" }
print *, "Foo bar"&
        &, "Bar foo"
print *, "Foo bar"&
        , "Bar foo"

print '(&
    a)', 'Hello' ! { dg-warning "Missing '&' in continued character constant" }
print '(&
   &a)', 'Hello'
print '('&
   &//'a)', 'Hello'
print '('&
   // "a)", 'Hello'
end
