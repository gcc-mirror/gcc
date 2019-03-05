/*
TEST_OUTPUT:
---
fail_compilation/fail343.d(22): Error: function fail343.TimedApp.run cannot override final function I.fail343.Timer.run
fail_compilation/fail343.d(22): Error: function fail343.TimedApp.run cannot override final function Application.fail343.Application.run
---
*/

interface Timer
{
    final void run() { }
}

interface I : Timer { }
interface Application
{
    final void run() { }
}
class TimedApp : I, Application
{
    // cannot define run()
    void run() { }
}
