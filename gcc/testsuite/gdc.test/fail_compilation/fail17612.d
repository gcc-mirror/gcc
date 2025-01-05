/* TEST_OUTPUT:
---
fail_compilation/fail17612.d(16): Error: undefined identifier `string`
fail_compilation/fail17612.d(19): Error: `TypeInfo` not found. object.d may be incorrectly installed or corrupt.
fail_compilation/fail17612.d(19):        dmd might not be correctly installed. Run 'dmd -man' for installation instructions.
fail_compilation/fail17612.d(19):        config file: not found
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17612

module object;

class Object
{
    string toString();
}

class TypeInfo {}
