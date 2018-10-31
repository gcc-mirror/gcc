/*
TEST_OUTPUT:
---
fail_compilation/fail11748.d(12): Error: expression my_function(0) is void and has no value
---
*/

void main()
{
    enum my_template(alias T) = T.stringof;
    void my_function(int i) { }
    my_template!(my_function(0));
}
