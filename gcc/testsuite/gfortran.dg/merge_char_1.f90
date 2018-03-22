! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 15327
! The merge intrinsic didn't work for strings
character*2 :: c(2)
logical :: ll(2)

ll = (/ .TRUE., .FALSE. /)
c = merge( (/ "AA", "BB" /), (/ "CC", "DD" /), ll )
if (c(1).ne."AA" .or. c(2).ne."DD") STOP 1

c = ""
c = merge( (/ "AA", "BB" /), (/ "CC", "DD" /), (/ .TRUE., .FALSE. /) )
if (c(1).ne."AA" .or. c(2).ne."DD") STOP 2
end
