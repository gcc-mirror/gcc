! { dg-do compile } 
program foo
   logical a(2)
   real x
   call random_init(1., .false.) ! { dg-error "must be LOGICAL" }
   call random_init(.true., 1)   ! { dg-error "must be LOGICAL" }
   call random_number(x)
   a = .true.
   call random_init(a, .false.) ! { dg-error "must be a scalar" }
   call random_init(.false., a) ! { dg-error "must be a scalar" }
end program foo
