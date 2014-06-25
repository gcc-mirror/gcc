! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! As SOURCE is INTENT(INOUT), it must be definable,
! cf. J3/14-147
!

intrinsic :: co_sum, co_min, co_max
integer :: vec(3), idx(3)

call co_sum(vec(idx)) ! { dg-error "Argument 'A' with INTENT\\(INOUT\\) at .1. of the intrinsic subroutine co_sum shall not have a vector subscript" }
call co_min(vec([1,3,2])) ! { dg-error "Argument 'A' with INTENT\\(INOUT\\) at .1. of the intrinsic subroutine co_min shall not have a vector subscript" }
call co_sum(vec([1,1,1])) ! { dg-error "Elements with the same value at .1. and .2. in vector subscript in a variable definition context \\(argument 'A' with INTENT\\(INOUT\\)\\)" }
end
