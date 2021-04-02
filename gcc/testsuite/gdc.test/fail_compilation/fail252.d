/*
TEST_OUTPUT:
---
fail_compilation/fail252.d(13): Error: cannot implicitly override base class method `fail252.Timer.Task.run` with `fail252.Timer.__anonclass1.run`; add `override` attribute
---
*/
class Timer {
    abstract class Task {
        public abstract void run();
    }
    private Task IDLE = new class() Task {
        int d;
        public void run(){
        }
    };
}
