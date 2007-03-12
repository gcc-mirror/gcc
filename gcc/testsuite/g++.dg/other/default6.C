// PR c++/30108

class BaseRobot {
  typedef void (BaseRobot::*PseudoState)(void);
  typedef PseudoState STATE;
  STATE initial ();
  int ready ();
  STATE stpOtherTask ();
  STATE commonEventProcessing (STATE pIdleTarget=(STATE)&BaseRobot::ready);
};
BaseRobot::STATE BaseRobot::initial ()
{
  return commonEventProcessing ();
}
BaseRobot::STATE BaseRobot::stpOtherTask ()
{
  return commonEventProcessing ();
}
