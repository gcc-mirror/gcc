! { dg-do  run }
! PR 48890, PR 83823
! Test fix for wrong length in parameters. Original test cases
! by mhp77 (a) gmx.at and Harald Anlauf.

program gfcbug145
  implicit none
  type t_obstyp
    character(len=8) :: name
  end type t_obstyp
  type (t_obstyp) ,parameter :: obstyp(*)= &
     [ t_obstyp ('SYNOP' ), &
       t_obstyp ('DRIBU' ), &
       t_obstyp ('TEMP'  ), &
       t_obstyp ('RADAR' )  ]
  logical :: mask(size(obstyp)) = .true.
  character(len=100) :: line
  type (t_obstyp), parameter :: x = t_obstyp('asdf')

  write(line,'(20(a8,:,"|"))') pack (obstyp% name, mask)
  if (line /= 'SYNOP   |DRIBU   |TEMP    |RADAR') STOP 1
  write (line,'("|",A,"|")') x
  if (line /= "|asdf    |") STOP 2
end program gfcbug145
