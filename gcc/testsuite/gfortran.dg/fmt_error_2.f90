! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR 33269: we used to not simplify format strings before checking if
! they were valid, leading to a missed error.

IMPLICIT CHARACTER*5 (h-z)

CHARACTER*5 f
CHARACTER*5 bad, good
parameter(bad="a", good="(a)")

PRINT ('a'), "hello" ! { dg-error "Missing leading left parenthesis in format string" }
WRITE (*, ("a")) "error"  ! { dg-error "Missing leading left parenthesis in format string" }

PRINT 'a', "hello" ! { dg-error "Missing leading left parenthesis in format string" }
WRITE (*, "a") "error"  ! { dg-error "Missing leading left parenthesis in format string" }
WRITE (*, bad) "error"  ! { dg-error "Missing leading left parenthesis in format string" }

PRINT 'a' // ', a', "err", "or"   ! { dg-error "Missing leading left parenthesis in format string" }

PRINT '(' // 'a'  ! { dg-error "Unexpected end of format string in format string" }

! the following are ok
PRINT "(2f5.3)", bar, foo
PRINT ' (a)', "hello"
WRITE (*, " ((a))") "hello"
print "(a" // ")", "all is fine"
print good, "great"

! verify that we haven't broken non-constant expressions
f = "(f5.3)"
print f, 3.14159
print (f), 2.71813
print implicitly_typed, "something"
write (*, implicitly_typed_as_well) "something else"
END
