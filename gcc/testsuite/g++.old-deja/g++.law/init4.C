// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed initialization
class Time;
class TimeNote;

class SvTime
{
public:
    static TimeNote *time_events = 0;// { dg-error "" } .*
};

SvTime CurrentTime = {0};// { dg-error "" } 
