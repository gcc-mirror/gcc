! { dg-do compile }
! PR103506 ICE in gfc_free_namespace. ice-on-invalid
! Test case from the PR.
module m ! { dg-error "is already being used as a MODULE" }
stop ! { dg-error "Unexpected STOP statement in MODULE" }
end
program p
call m ! { dg-error "is already being used as a MODULE" }
end
