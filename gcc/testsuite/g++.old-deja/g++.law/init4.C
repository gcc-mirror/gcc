// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed initialization
class Time;
class TimeNote;

class SvTime
{
public:
    static TimeNote *time_events = 0;// ERROR - .*
};

SvTime CurrentTime = {0};// ERROR - 
