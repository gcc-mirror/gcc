// PR c++/40145
// { dg-do compile }
// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }

struct EditorInternalCommand { };

static void createCommandMap()
{
    struct CommandEntry { EditorInternalCommand command; };
}
