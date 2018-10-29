
version (D_InlineAsm_X86)
    version = TestInlineAsm;
else version (D_InlineAsm_X86_64)
    version = TestInlineAsm;
else version (GNU)
    version = TestInlineAsm;
else
    pragma(msg, "Inline asm not supported, not testing.");

version (TestInlineAsm)
{
    void testInlineAsm()
    {
        version (GNU)
        {
        L1:
            asm { ""; }
            asm { "" : : : : L1, L2; }
        L2:
            asm { ""; }
        }
        else
        {
            asm
            {
		L1:
			nop;
			nop;
			nop;
			nop;
			
			mov EAX, dword ptr L1; // Check back references
			mov EAX, dword ptr L2; // Check forward references
			mov EAX, dword ptr DS:L1; // Not really useful in standard use, but who knows.
			mov EAX, dword ptr FS:L2; // Once again, not really useful, but it is valid.
			mov EAX, dword ptr CS:L1; // This is what the first test case should implicitly be.
			
		L2:
			nop;
			nop;
			nop;
			nop;
			
            }
        }
    }
}
