! { dg-do run }
! Assign a label to a dummy argument.
! Option passed to avoid excess errors from obsolete warning
! { dg-options "-w" }

subroutine s1 (a)
integer a
assign 777 to a
go to a
777 continue
end
program test
call s1 (1)
end

