! { dg-do run }
program L
   if (and(.TRUE._1, .TRUE._1) .neqv. .true.) call abort
   if (or(.TRUE._1, .TRUE._1) .neqv. .true.) call abort
   if (xor(.TRUE._1, .TRUE._1) .neqv. .false.) call abort
end program L

