// REQUIRED_ARGS: -O

void _d_critical_term()
{
    for (auto p = head; p; p = p.next)
        destroyMutex(p.i);
}

shared S* head;

struct S
{
    S* next;
    int i;
}

void destroyMutex(int i);

struct Mutex { int i; }
