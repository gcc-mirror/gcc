*> { dg-do compile }
*> { dg-options "-g0 -O2 -ffree-form" }
*> { dg-final { scan-assembler-not "_gdb_file_connectors" } }
*> { dg-final { scan-assembler-not "_gdb_file_connector_" } }
*> { dg-final { scan-assembler-not "__gg__debug_file_v1" } }
identification division.
program-id. debug-files.
environment division.
input-output section.
file-control.
    select data-file assign to "debug-files.dat".
data division.
file section.
fd data-file external.
01 data-record pic x(8).
procedure division.
    goback.
end program debug-files.
identification division.
program-id. empty-program.
procedure division.
    goback.
end program empty-program.
