! { dg-do run }
! PR 15327
! The merge intrinsic didn't work for strings
character*2 :: c(2)
c = merge( (/ "AA", "BB" /), (/ "CC", "DD" /), (/ .TRUE., .FALSE. /) )
if (c(1).ne."AA" .or. c(2).ne."DD") call abort ()
end
