// https://issues.dlang.org/show_bug.cgi?id=18821

align(1) struct epoll_event
{
    void* ptr;
}
template isAllZeroBits(epoll_event value) {}
alias isInitAllZeroBits = isAllZeroBits!(epoll_event.init);

epoll_event e = { null };
