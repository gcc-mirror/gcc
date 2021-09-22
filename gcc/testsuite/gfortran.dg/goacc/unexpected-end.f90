! PR fortran/102313

!$acc end ATOMIC  ! { dg-error "Unexpected !.ACC END ATOMIC" }

!$acc end DATA  ! { dg-error "Unexpected !.ACC END DATA" }

!$acc end HOST_DATA  ! { dg-error "Unexpected !.ACC END HOST_DATA" }

!$acc end KERNELS  ! { dg-error "Unexpected !.ACC END KERNELS" }

!$acc end KERNELS LOOP  ! { dg-error "Unexpected !.ACC END KERNELS LOOP" }

!$acc end LOOP  ! { dg-error "Unexpected !.ACC END LOOP" }

!$acc end PARALLEL  ! { dg-error "Unexpected !.ACC END PARALLEL" }

!$acc end PARALLEL LOOP  ! { dg-error "Unexpected !.ACC END PARALLEL LOOP" }

!$acc end SERIAL  ! { dg-error "Unexpected !.ACC END SERIAL" }

!$acc end SERIAL LOOP  ! { dg-error "Unexpected !.ACC END SERIAL LOOP" }

!$acc end EUPHORBIA LATHYRIS  ! { dg-error "Unclassifiable OpenACC directive" }

end
