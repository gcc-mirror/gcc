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
