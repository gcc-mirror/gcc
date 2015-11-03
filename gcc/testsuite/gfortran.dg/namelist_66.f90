! { dg-do run { target fd_truncate } }
! PR46010 Failure to read these two examples of namelists
type ptracer
   character(len = 2)  :: sname
   logical              :: lini
end type ptracer
type(ptracer) , dimension(3) :: tracer
namelist/naml1/  tracer

type qptracer
   character(len = 20)  :: sname  = ""!: short name
   character(len = 45 ) :: lname  = ""!: long name
   character(len = 20 ) :: sunit  = "" !: unit
   logical              :: lini   !: read in a file or not
   logical              :: lsav   !: ouput the tracer or not 
end type qptracer
type(qptracer) , dimension(3) :: qtracer
namelist/naml2/  qtracer

open (99, file='nml_66.dat', status="replace")
write(99,*) "&naml1"
write(99,*) "   tracer(1)   = 'aa', .true."
write(99,*) "   tracer(2)   = 'bb', .true."
write(99,*) "   tracer(3)   = 'cc', .true."
write(99,*) "/"
rewind(99)
read (99, nml=naml1)
write (*, nml=naml1)
rewind(99)
write(99,*) "&naml2     !   just some stuff"
write(99,*) "   qtracer(1)   = 'dic     ' , 'dissolved inorganic concentration      ',  'mol-c/l' ,  .true.     ,  .true.,"
write(99,*) "   qtracer(2)   = 'alkalini' , 'total alkalinity concentration         ',  'eq/l '   ,  .true.     ,  .true.,"
write(99,*) "/"
rewind(99)
read (99, nml=naml2)
write (*, nml=naml2)
rewind(99)

close (99, status="delete")
end
