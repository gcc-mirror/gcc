struct MsgProc1 { mixin MsgMixin; }
struct MsgProc2 { mixin MsgMixin; }

struct MsgHeader {}

template MsgMixin()
{
    mixin(mixinMembers!(MsgHeader.init));
}

string mixinMembers(T ...)()
{
    struct Op {}
    return null;
}
