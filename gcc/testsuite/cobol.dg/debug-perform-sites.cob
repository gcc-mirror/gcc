*> { dg-do compile }
*> { dg-options "-g -O2 -ffree-form" }
*> { dg-final { scan-assembler "_gdb_perform_sites" } }
*> { dg-final { scan-assembler "_gdb_perform_invocation" } }
*> { dg-final { scan-assembler "__gg__debug_pseudo_return_bookmark" } }
*> { dg-final { scan-assembler "_gdb_perform_return_counter" } }
identification division.
program-id. debug-perform.
data division.
working-storage section.
01 total binary-long value 0.
procedure division.
    perform worker 2 times
    goback.
worker-section section.
worker.
    add 1 to total
    exit paragraph.
end program debug-perform.
