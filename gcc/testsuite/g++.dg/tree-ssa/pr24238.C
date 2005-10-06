/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct SDL_Rect {
    unsigned short w, h;
}SDL_Rect;
SDL_Rect *location();
SDL_Rect inner_location()
{
    SDL_Rect r = *location();
    r.w -= 1;
    return r;
}
