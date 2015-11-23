! { dg-do run }
! PR52251 Tabs with advance = 'no'
write( *, '( t25 )', advance = 'no' )
write( *, '( "hello" )' ) ! { dg-output "                       hello(\n|\r\n|\r)" }
end

