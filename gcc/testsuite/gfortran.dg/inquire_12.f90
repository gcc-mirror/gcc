! { dg-do compile }
! PR34722 ICE: left-over "@iostat" variable polutes namespace
program gamsanal
implicit none
character :: tmp
integer iodict
logical dicexist
inquire(unit=iodict, exist=dicexist)
end

subroutine inventnames()
implicit none
end subroutine