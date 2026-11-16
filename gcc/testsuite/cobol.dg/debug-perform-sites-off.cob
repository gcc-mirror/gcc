*> { dg-do compile }
*> { dg-options "-g0 -O2 -ffree-form" }
*> { dg-final { scan-assembler-not "_gdb_perform_" } }
*> { dg-final { scan-assembler-not "__gg__debug_pseudo_return_bookmark" } }
*> { dg-final { scan-assembler "__gg__pseudo_return_bookmark" } }
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
