// { dg-do assemble  }
// GROUPS passed old-abort
class D_Interval;

class Date
{
 public:
  Date(const D_Interval*,const Date&);
private:
  const D_Interval* interval;
};

class Time_Interval
{
 public:
   Time_Interval(const Date& start,const Date& stop);
   const Date& Start() const { return start; }
   const Date& Stop() const { return stop; }
 private:
   Date start;
   Date stop;
};

class Dated_Data
{
 public:
   Dated_Data(const Time_Interval& dates);
   virtual ~Dated_Data();
   Time_Interval Dates() const { return dates; }
 private:
   Time_Interval dates;
};

class Raw_Data : public Dated_Data
{
 public:
   Raw_Data(const Dated_Data *source,const D_Interval& period);
};

Raw_Data::Raw_Data(const Dated_Data *source,const D_Interval& period)
     : Dated_Data(Time_Interval(Date(&period,source->Dates().Start()),
				Date(&period,source->Dates().Stop())))
{
}
