// Test case from: <ncm-nospam@cantrip.org>
// Reduced by <bangerth@dealii.org>
// The problem was that g++ was ICE because
// it was deferring an NULL pointer because
// it should have been taking the operand 1
// instead of the chain in finish_fname_decls.


void fun()
try
{
    __FUNCTION__;
}
catch (...) {}
