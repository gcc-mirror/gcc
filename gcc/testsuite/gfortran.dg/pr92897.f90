! { dg-do compile }
! { dg-options "-fcoarray=single" }
! Test contributed by Gerhard Steinmetz
type(t) function f()                    ! { dg-error "has not been declared" }
   dimension :: t(1,2,1,2,1,2,1,2)
   codimension :: t[1,2,1,2,1,2,1,*]    ! { dg-error "rank \\+ corank of" }
end
! { dg-prune-output "which has not been defined" }
