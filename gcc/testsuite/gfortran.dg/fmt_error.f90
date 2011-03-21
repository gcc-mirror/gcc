! { dg-do compile }
! PR32545 Give compile error not warning for wrong edit format statements.
read (5,'(i0)') i ! { dg-error "Positive width required in format" }
end
