
void funcDefault(void);

#pragma attribute(push, nothrow)
void funcNothrow(void);
#pragma attribute(pop)

#pragma attribute(push, nogc)
void funcNogc(void);
#pragma attribute(pop)

#pragma attribute(push, pure)
void funcPure(void);
#pragma attribute(pop)

#pragma attribute(push, nothrow, nogc)
void funcNothrowNogc(void);
#pragma attribute(pop)

void funcDefault2(void);

#pragma attribute(push, nothrow)
#pragma attribute(push, nogc)
void funcNothrowNogc2(void);
#pragma attribute(pop)
void funcNothrow2(void);
#pragma attribute(pop)

#pragma attribute(push, nothrow)
void funcWithCallback(void (*f)(void));
struct Callbacks
{
    void (*f)(void);
};
#pragma attribute(pop)
