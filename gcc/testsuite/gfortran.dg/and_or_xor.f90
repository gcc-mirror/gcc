! { dg-do run }
program L
   if (and(.TRUE._1, .TRUE._1) .neqv. .true.) STOP 1
   if (or(.TRUE._1, .TRUE._1) .neqv. .true.) STOP 2
   if (xor(.TRUE._1, .TRUE._1) .neqv. .false.) STOP 3
end program L

