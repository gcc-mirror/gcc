! { dg-do run }
! PR56660  Fails to read NAMELIST with certain form array syntax
type ptracer
   character(len = 2)  :: sname
   logical             :: lini
end type ptracer

type(ptracer) , dimension(3) :: tracer
namelist/naml1/  tracer

tracer(:) = ptracer('XXX', .false.)

open (99, file='nml_82.dat', status="replace")
write(99,*) "&naml1"
!write(99,*) "   tracer(2)   = 'bb' , .true."
write(99,*) "   tracer(:)   = 'aa' , .true."
write(99,*) "   tracer(2)   = 'bb' , .true."
write(99,*) "/"
rewind(99)

read (99, nml=naml1)
close (99, status="delete")

if (tracer(1)%sname.ne.'aa') call abort()
if (.not.tracer(1)%lini) call abort()
if (tracer(2)%sname.ne.'bb') call abort()
if (.not.tracer(2)%lini) call abort()
if (tracer(3)%sname.ne.'XX') call abort()
if (tracer(3)%lini) call abort()

!write (*, nml=naml1)

end
