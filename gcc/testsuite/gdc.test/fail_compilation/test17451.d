/* TEST_OUTPUT:
---
fail_compilation/test17451.d(22): Error: undefined identifier `allocator`
fail_compilation/test17451.d(23): Error: `long` has no effect in expression `false`
fail_compilation/test17451.d(30): Error: variable test17451.HashMap!(ThreadSlot).HashMap.__lambda2.v size of type ThreadSlot is invalid
fail_compilation/test17451.d(44): Error: template instance test17451.HashMap!(ThreadSlot) error instantiating
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17451

interface ManualEvent {}

interface EventDriver {
        ManualEvent createManualEvent() ;
}

struct ArraySet(Key)
{
        ~this()
    {
                try allocator;
                catch false; // should never happen
        }
}

struct HashMap(TValue)
{
        alias Value = TValue;
        static if ({ Value v; }) {}
}

struct Task {}

class Libevent2Driver : EventDriver {
        Libevent2ManualEvent createManualEvent() {}
}

struct ThreadSlot {
        ArraySet!Task tasks;
}

class Libevent2ManualEvent {
        HashMap!ThreadSlot m_waiters;
}
