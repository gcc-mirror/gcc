/* { dg-do compile } */

typedef struct {
  double x, y;
} pointf;
struct {
  pointf focus;
  double zoom;
  pointf devscale;
  char button;
  pointf oldpointer;
} gvevent_motion_job;
char gvevent_motion_job_4;
double gvevent_motion_pointer_1, gvevent_motion_pointer_0;
void gvevent_motion() {
  double dx = (gvevent_motion_pointer_0 - gvevent_motion_job.oldpointer.x) /
              gvevent_motion_job.devscale.x,
         dy = (gvevent_motion_pointer_1 - gvevent_motion_job.oldpointer.y) /
              gvevent_motion_job.devscale.y;
  if (dx && dy < .0001)
    return;
  switch (gvevent_motion_job_4)
  case 2: {
    gvevent_motion_job.focus.x -= dy / gvevent_motion_job.zoom;
    gvevent_motion_job.focus.y += dx / gvevent_motion_job.zoom;
  }
}
