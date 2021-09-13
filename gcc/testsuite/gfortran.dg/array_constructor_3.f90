! { dg-do compile }
! Check that empty array constructors are rejected
program hum
    print *, (//) ! { dg-error "Empty array constructor" }
end program hum
