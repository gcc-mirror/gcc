! { dg-do compile }
!
! PR fortran/93366 - ICE on invalid, reject invalid use of NULL() as argument

program p
  print *, kind (null())                    ! { dg-error "NULL at" }
  print *, [ merge(null(), [1]   ,.true.) ] ! { dg-error "NULL at" }
  print *, [ merge([1]   , null(),.true.) ] ! { dg-error "NULL at" }
  print *, [ merge(null(), null(),.true.) ] ! { dg-error "NULL at" }
  print *, shape (null())                   ! { dg-error "NULL at" }
  print *, sizeof (null())                  ! { dg-error "NULL at" }
  print *, spread (null(),1,1)              ! { dg-error "NULL at" }
  print *, transfer ( 1 , null())           ! { dg-error "NULL at" }
  print *, transfer ([1], null())           ! { dg-error "NULL at" }
  print *, transfer (null(), 1)             ! { dg-error "NULL at" }
  print *, transfer (null(), [1])           ! { dg-error "NULL at" }
  print *, transfer (null(), null())        ! { dg-error "NULL at" }
end
