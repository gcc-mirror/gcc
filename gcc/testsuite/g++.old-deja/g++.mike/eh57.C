// Build don't link:
// Special g++ Options: -fno-exceptions

class Calendar_Time {
public:
  ~Calendar_Time ();
  int operator <= (const Calendar_Time& t) const;
};

class Temporal_Model_Interval {
public:
  Calendar_Time start_time ();
};

int intersects_p (Temporal_Model_Interval* i1, Temporal_Model_Interval* i2) {
  return ((i1->start_time() <= i2->start_time())
	  || (i1->start_time() <= i2->start_time()));
}
