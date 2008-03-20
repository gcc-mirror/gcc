! { dg-do run }
! PR35627 Namelist read problem with short logical followed by read real
program test
  implicit none
    LOGICAL :: nlco(200)  ! (1:nbeam)
    REAL(kind=8):: xlbtna(200)  ! (1:nbeam)
  NAMELIST/nbdrive_naml/ nlco, xlbtna
    INTEGER :: nbshapa(200)  ! (1:nbeam)
  NAMELIST/nbdrive_naml/ nbshapa
  nlco = .false.
  xlbtna = 0.0_8
  nbshapa = 0
  open(10, file='t.nml')
  write(10,'(a)') "&nbdrive_naml"
  write(10,'(a)') "nlco = 4*T,"
  write(10,'(a)') "xlbtna = 802.8, 802.8, 802.8, 802.8"
  write(10,'(a)') "nbshapa = 4*1"
  write(10,'(a)') "/"
  rewind(10)
  read(10, nbdrive_naml)
  !write(*,nbdrive_naml)
  close(10, status="delete")
end program test
